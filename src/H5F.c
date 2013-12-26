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

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5F__init_pub_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5VLnative.h" 	/* Native Plugin                        */
#include "H5VLprivate.h"	/* VOL plugins				*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

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

/* File ID class */
static const H5I_class_t H5I_FILE_CLS[1] = {{
    H5I_FILE,			/* ID class value */
    0,				/* Class flags */
    0,				/* # of reserved IDs for class */
    NULL,                       /* Callback routine for closing objects of this class */
    (H5I_free2_t)H5F_close_file /* Callback routine for closing auxilary objects of this class */
}};


/*--------------------------------------------------------------------------
NAME
   H5F__init_pub_interface -- Initialize interface-specific information
USAGE
    herr_t H5F__init_pub_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5F_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5F__init_pub_interface(void)
{
    herr_t          ret_value                = SUCCEED;   /* Return value */
    FUNC_ENTER_STATIC

    /*
     * Initialize the atom group for the file IDs.
     */
    if(H5I_register_type(H5I_FILE_CLS) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to initialize interface")

    ret_value = H5F_init();
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__init_pub_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5F_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 16, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

#if 0
    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Initialize the atom group for the file IDs.
     */
    if(H5I_register_type(H5I_FILE_CLS) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_init_interface() */
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_init() */


/*-------------------------------------------------------------------------
 * Function:	H5F_term_interface
 *
 * Purpose:	Terminate this interface: free all memory and reset global
 *		variables to their initial values.  Release all ID groups
 *		associated with this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				have affected other interfaces; zero
 *				otherwise.
 *
 *		Failure:        Never fails.
 *
 * Programmer:	Robb Matzke
 *              Friday, February 19, 1999
 *
 *-------------------------------------------------------------------------
 */
int
H5F_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if((n = H5I_nmembers(H5I_FILE)) != 0) {
            H5I_clear_type(H5I_FILE, FALSE, FALSE);
	} else {
            /* Make certain we've cleaned up all the shared file objects */
            H5F_sfile_assert_num(0);

	    H5I_dec_type_ref(H5I_FILE);
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* H5F_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_create_plist
 *
 * Purpose:	Get an atom for a copy of the file-creation property list for
 *		this file. This function returns an atom with a copy of the
 *		properties used to create a file.
 *
 * Return:	Success:	template ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_create_plist(hid_t file_id)
{
    H5VL_t     *vol_plugin = NULL;
    void       *obj = NULL;
    hid_t       ret_value;        /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", file_id);

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(H5VL_file_get(obj, vol_plugin, H5VL_FILE_GET_FCPL, H5AC_dxpl_id, H5_EVENT_STACK_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file creation properties")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_create_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_access_plist
 *
 * Purpose:	Returns a copy of the file access property list of the
 *		specified file.
 *
 *              NOTE: Make sure that, if you are going to overwrite
 *              information in the copied property list that was
 *              previously opened and assigned to the property list, then
 *              you must close it before overwriting the values.
 *
 * Return:	Success:	Object ID for a copy of the file access
 *				property list.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, February 18, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_access_plist(hid_t file_id)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    hid_t ret_value;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", file_id);

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    if(H5VL_file_get(obj, vol_plugin, H5VL_FILE_GET_FAPL, H5AC_dxpl_id, H5_EVENT_STACK_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file creation properties")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_access_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_all_count_cb
 *
 * Purpose:     Get counter of all object types currently open.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:  Mohamad Chaarawi
 *              May 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5F_get_all_count_cb(void UNUSED *obj_ptr, hid_t UNUSED obj_id, void *key)
{
    H5F_trav_obj_cnt_t *udata = (H5F_trav_obj_cnt_t *)key;
    int                ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    *(udata->obj_count) = *(udata->obj_count)+1; 

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F_get_all_count_cb */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_obj_count
 *
 * Purpose:	Public function returning the number of opened object IDs
 *		(files, datasets, groups and datatypes) in the same file.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:	Raymond Lu
 *		Wednesday, Dec 5, 2001
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_obj_count(hid_t file_id, unsigned types)
{
    ssize_t ret_value = 0;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("Zs", "iIu", file_id, types);

    if(file_id != (hid_t)H5F_OBJ_ALL) {
        H5VL_t     *vol_plugin;
        void       *obj;

        /* get the file object */
        if(NULL == (obj = (void *)H5I_object_verify(file_id, H5I_FILE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file id")
        /* get the plugin pointer */
        if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

        if(H5VL_file_get(obj, vol_plugin, H5VL_FILE_GET_OBJ_COUNT, H5AC_dxpl_id, 
                         H5_EVENT_STACK_NULL, types, &ret_value) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get object count in file(s)")
    }
    /* iterate over all open files and get the obj count for each */
    else {
        H5F_trav_obj_cnt_t udata;

        udata.obj_count = &ret_value;
        udata.types = types | H5F_OBJ_LOCAL;

        if(types & H5F_OBJ_FILE) {
            if(H5I_iterate(H5I_FILE, H5F_get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_DATASET) {
            if(H5I_iterate(H5I_DATASET, H5F_get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_GROUP) {
            if(H5I_iterate(H5I_GROUP, H5F_get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_DATATYPE) {
            if(H5I_iterate(H5I_DATATYPE, H5F_get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_ATTR) {
            if(H5I_iterate(H5I_ATTR, H5F_get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_obj_count() */


/*-------------------------------------------------------------------------
 * Function:	H5F_get_all_ids_cb
 *
 * Purpose:     Get ids of all object types currently open.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:  Mohamad Chaarawi
 *              May 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5F_get_all_ids_cb(void UNUSED *obj_ptr, hid_t obj_id, void *key)
{
    H5F_trav_obj_ids_t *udata = (H5F_trav_obj_ids_t *)key;
    int                ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(*udata->obj_count >= udata->max_objs)
        HGOTO_DONE(H5_ITER_STOP);

    udata->oid_list[*udata->obj_count] = obj_id;
    *(udata->obj_count) = *(udata->obj_count)+1; 

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F_get_all_ids_cb */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_object_ids
 *
 * Purpose:	Public function to return a list of opened object IDs.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *              Raymond Lu
 *              24 September 2008
 *              Changed the return value to ssize_t and MAX_OBJTS to size_t to
 *              accommadate potential large number of objects.
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *oid_list)
{
    ssize_t   ret_value = 0;        /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("Zs", "iIuz*i", file_id, types, max_objs, oid_list);

    if(0 == (types & H5F_OBJ_ALL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not an object type")
    if(!oid_list)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "object ID list is NULL")

    /* Check arguments */
    if(file_id != (hid_t)H5F_OBJ_ALL) {
        H5VL_t     *vol_plugin;
        void       *obj;
        /* get the plugin pointer */
        if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
        /* get the file object */
        if(NULL == (obj = (void *)H5I_object(file_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

        if(H5VL_file_get(obj, vol_plugin, H5VL_FILE_GET_OBJ_IDS, H5AC_dxpl_id, H5_EVENT_STACK_NULL, 
                         types, max_objs, oid_list, &ret_value) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get object count in file(s)")
    }
    /* iterate over all open files and get the obj count for each */
    else if (oid_list && max_objs){
        H5F_trav_obj_ids_t udata;

        //udata.types = types | H5F_OBJ_LOCAL;
        udata.max_objs = max_objs;
        udata.oid_list = oid_list;
        udata.obj_count = &ret_value;

        if(types & H5F_OBJ_FILE) {
            if(H5I_iterate(H5I_FILE, H5F_get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_DATASET) {
            if(H5I_iterate(H5I_DATASET, H5F_get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_GROUP) {
            if(H5I_iterate(H5I_GROUP, H5F_get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_DATATYPE) {
            if(H5I_iterate(H5I_DATATYPE, H5F_get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
        if(types & H5F_OBJ_ATTR) {
            if(H5I_iterate(H5I_ATTR, H5F_get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)");
        }
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_obj_ids() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_vfd_handle
 *
 * Purpose:     Returns a pointer to the file handle of the low-level file
 *              driver.
 *
 * Return:      Success:        non-negative value.
 *              Failure:        negative.
 *
 * Programmer:  Raymond Lu
 *              Sep. 16, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t      ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ii**x", file_id, fapl, file_handle);

    /* Check args */
    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file handle pointer")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if((ret_value = H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_VFD_HANDLE, 
                                       H5AC_dxpl_id, H5_EVENT_STACK_NULL, file_handle, fapl)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_vfd_handle() */


/*-------------------------------------------------------------------------
 * Function:	H5Fis_accessible
 *
 * Purpose:	Check if the file can be opened with the given fapl.
 *
 * Return:	Success:	TRUE/FALSE
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              June 2012
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Fis_accessible(const char *name, hid_t fapl_id)
{
    htri_t	ret_value = TRUE;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("t", "*si", name, fapl_id);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "no file name specified")

    /* Check the file access property list */
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    if(H5VL_file_misc(NULL, NULL, H5VL_FILE_IS_ACCESSIBLE, H5AC_dxpl_id, H5_EVENT_STACK_NULL, 
                      fapl_id, name, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fis_accessible() */


/*-------------------------------------------------------------------------
 * Function:	H5Fcreate
 *
 * Purpose:	This is the primary function for creating HDF5 files . The
 *		flags parameter determines whether an existing file will be
 *		overwritten or not.  All newly created files are opened for
 *		both reading and writing.  All flags may be combined with the
 *		bit-wise OR operator (`|') to change the behavior of the file
 *		create call.
 *
 *		The more complex behaviors of a file's creation and access
 *		are controlled through the file-creation and file-access
 *		property lists.  The value of H5P_DEFAULT for a template
 *		value indicates that the library should use the default
 *		values for the appropriate template.
 *
 * See also:	H5Fpublic.h for the list of supported flags. H5Ppublic.h for
 * 		the list of file creation and file access properties.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    void    *file = NULL;              /*file token from VOL plugin */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    hid_t    ret_value;	        /*return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("i", "*sIuii", filename, flags, fcpl_id, fapl_id);

    /* Check/fix arguments */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    /* In this routine, we only accept the following flags:
     *          H5F_ACC_EXCL, H5F_ACC_TRUNC and H5F_ACC_DEBUG
     */
    if(flags & ~(H5F_ACC_EXCL | H5F_ACC_TRUNC | H5F_ACC_DEBUG))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags")
    /* The H5F_ACC_EXCL and H5F_ACC_TRUNC flags are mutually exclusive */
    if((flags & H5F_ACC_EXCL) && (flags & H5F_ACC_TRUNC))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mutually exclusive flags for file creation")

    /* Check file creation property list */
    if(H5P_DEFAULT == fcpl_id)
        fcpl_id = H5P_FILE_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fcpl_id, H5P_FILE_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file create property list")

    /* Check the file access property list */
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* create a new file or truncate an existing file through the VOL */
    if(NULL == (file = H5VL_file_create(&vol_plugin, filename, flags, fcpl_id, fapl_id, 
                                        H5AC_dxpl_id, H5_EVENT_STACK_NULL)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    /* Get an atom for the file with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register2(H5I_FILE, file, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fcreate() */


/*-------------------------------------------------------------------------
 * Function:	H5Fopen
 *
 * Purpose:	This is the primary function for accessing existing HDF5
 *		files.  The FLAGS argument determines whether writing to an
 *		existing file will be allowed or not.  All flags may be
 *		combined with the bit-wise OR operator (`|') to change the
 *		behavior of the file open call.  The more complex behaviors
 *		of a file's access are controlled through the file-access
 *		property list.
 *
 * See Also:	H5Fpublic.h for a list of possible values for FLAGS.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *	  	Robb Matzke, 1997-07-18
 *		File struct creation and destruction is through H5F_new() and
 *		H5F_dest(). Reading the root symbol table entry is done with
 *		H5G_decode().
 *
 *  		Robb Matzke, 1997-09-23
 *		Most of the work is now done by H5F_open() since H5Fcreate()
 *		and H5Fopen() originally contained almost identical code.
 *
 *	 	Robb Matzke, 1998-02-18
 *		Added better error checking for the flags and the file access
 *		property list.  It used to be possible to make the library
 *		dump core by passing an object ID that was not a file access
 *		property list.
 *
 * 		Robb Matzke, 1999-08-02
 *		The file access property list is passed to the H5F_open() as
 *		object IDs.
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen(const char *filename, unsigned flags, hid_t fapl_id)
{
    void    *file = NULL;              /*file token from VOL plugin */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    hid_t   ret_value;	        /*return value			*/

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "*sIui", filename, flags, fapl_id);

    /* Check/fix arguments. */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    /* Reject undefined flags (~H5F_ACC_PUBLIC_FLAGS) and the H5F_ACC_TRUNC & H5F_ACC_EXCL flags */
    if((flags & ~H5F_ACC_PUBLIC_FLAGS) ||
            (flags & H5F_ACC_TRUNC) || (flags & H5F_ACC_EXCL))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file open flags")
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* Open the file through the VOL layer */
    if(NULL == (file = H5VL_file_open(&vol_plugin, filename, flags, fapl_id, H5AC_dxpl_id, H5_EVENT_STACK_NULL)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    /* Get an atom for the file with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register2(H5I_FILE, file, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Fflush
 *
 * Purpose:	Flushes all outstanding buffers of a file to disk but does
 *		not remove them from the cache.  The OBJECT_ID can be a file,
 *		dataset, group, attribute, or named data type.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fflush(hid_t object_id, H5F_scope_t scope)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    H5I_type_t obj_type;
    H5VL_loc_params_t loc_params;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iFs", object_id, scope);

    obj_type = H5I_get_type(object_id);
    if(H5I_FILE != obj_type && H5I_GROUP != obj_type && H5I_DATATYPE != obj_type &&
       H5I_DATASET != obj_type && H5I_ATTR != obj_type) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    }

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(object_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5VL_get_object(object_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = obj_type;

    if((ret_value = H5VL_file_flush(obj, loc_params, vol_plugin, scope, H5AC_dxpl_id, H5_EVENT_STACK_NULL)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fflush() */


/*-------------------------------------------------------------------------
 * Function:	H5Fclose
 *
 * Purpose:	This function closes the file specified by FILE_ID by
 *		flushing all data to storage, and terminating access to the
 *		file through FILE_ID.  If objects (e.g., datasets, groups,
 *		etc.) are open in the file then the underlying storage is not
 *		closed until those objects are closed; however, all data for
 *		the file and the open objects is flushed.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclose(hid_t file_id)
{
    H5VL_t  *vol_plugin = NULL;
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check/fix arguments. */
    if(H5I_FILE != H5I_get_type(file_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information");
    /* set the event queue and dxpl IDs to be passed on to the VOL layer */
    vol_plugin->close_estack_id = H5_EVENT_STACK_NULL;
    vol_plugin->close_dxpl_id = H5AC_dxpl_id;

    /* Decrement reference count on atom.  When it reaches zero the file will be closed. */
    if(H5I_dec_app_ref(file_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

#if 0
    void    *file;              /*file token from VOL plugin */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (file = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Close the file through the VOL*/
    if((ret_value = H5VL_file_close(file, vol_plugin, H5AC_dxpl_id, H5_EVENT_STACK_NULL)) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

    H5MM_free(vol_plugin);
#endif

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclose() */


/*-------------------------------------------------------------------------
 * Function:	H5F_close_file
 *
 * Purpose:	Called when the ref count reaches zero on the file_id
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              June 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_close_file(void *file, H5VL_t *vol_plugin)
{
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Close the file through the VOL*/
    if((ret_value = H5VL_file_close(file, vol_plugin, vol_plugin->close_dxpl_id, 
                                    vol_plugin->close_estack_id)) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close file")
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_close_file() */


/*-------------------------------------------------------------------------
 * Function:	H5Freopen
 *
 * Purpose:	Reopen a file.  The new file handle which is returned points
 *		to the same file as the specified file handle.  Both handles
 *		share caches and other information.  The only difference
 *		between the handles is that the new handle is not mounted
 *		anywhere and no files are mounted on it.
 *
 * Return:	Success:	New file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 16, 1998
 *
 * Modifications:
 *              Quincey Koziol, May 14, 2002
 *              Keep old file's read/write intent in reopened file.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Freopen(hid_t file_id)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    void       *file;           /*file token from VOL plugin */
    hid_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", file_id);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_REOPEN, H5AC_dxpl_id, H5_EVENT_STACK_NULL, &file) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file")

    if (NULL == file)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file")

    /* Get an atom for the file with the VOL information as the auxilary struct*/
    if((ret_value = H5I_register2(H5I_FILE, file, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Freopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_intent
 *
 * Purpose:	Public API to retrieve the file's 'intent' flags passed
 *              during H5Fopen()
 *
 * Return:	Non-negative on success/negative on failure
 *
 * Programmer:	James Laird
 *		August 23, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_intent(hid_t file_id, unsigned *intent_flags)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", file_id, intent_flags);

    /* If no intent flags were passed in, exit quietly */
    if(intent_flags) {
        H5VL_t     *vol_plugin;
        void       *obj;

        /* get the plugin pointer */
        if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

        /* get the file object */
        if(NULL == (obj = (void *)H5I_object(file_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

        if((ret_value = H5VL_file_get(obj, vol_plugin, H5VL_FILE_GET_INTENT, H5AC_dxpl_id, H5_EVENT_STACK_NULL, intent_flags)) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file intent")
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_intent() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_freespace
 *
 * Purpose:     Retrieves the amount of free space in the file.
 *
 * Return:      Success:        Amount of free space for type
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Oct  6, 2003
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5Fget_freespace(hid_t file_id)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    hssize_t    ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("Hs", "i", file_id);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_FREE_SPACE, H5AC_dxpl_id, H5_EVENT_STACK_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file free space")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_freespace() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_filesize
 *
 * Purpose:     Retrieves the file size of the HDF5 file. This function
 *              is called after an existing file is opened in order
 *		to learn the true size of the underlying file.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  David Pitt
 *              david.pitt@bigpond.com
 *              Apr 27, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_filesize(hid_t file_id, hsize_t *size)
{
    H5VL_t     *vol_plugin;
    void       *file;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", file_id, size);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (file = (void *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    if((ret_value = H5VL_file_optional(file, vol_plugin, H5VL_FILE_GET_SIZE, 
				       H5AC_dxpl_id, H5_EVENT_STACK_NULL, size)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_filesize() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_file_image
 *
 * Purpose:     If a buffer is provided (via the buf_ptr argument) and is 
 *		big enough (size in buf_len argument), load *buf_ptr with
 *		an image of the open file whose ID is provided in the 
 *		file_id parameter, and return the number of bytes copied
 *		to the buffer.
 *
 *		If the buffer exists, but is too small to contain an image
 *		of the indicated file, return a negative number.
 *
 *		Finally, if no buffer is provided, return the size of the 
 *		buffer needed.  This value is simply the eoa of the target 
 *		file.
 *
 *		Note that any user block is skipped.
 *
 *		Also note that the function may not be used on files 
 *		opened with either the split/multi file driver or the
 *		family file driver.
 *
 *		In the former case, the sparse address space makes the 
 *		get file image operation impractical, due to the size of
 *		the image typically required.
 *
 *		In the case of the family file driver, the problem is
 *		the driver message in the super block, which will prevent
 *		the image being opened with any driver other than the
 *		family file driver -- which negates the purpose of the 
 *		operation.  This can be fixed, but no resources for 
 *		this now.
 *
 * Return:      Success:        Bytes copied / number of bytes needed.
 *              Failure:        negative value
 *
 * Programmer:  John Mainzer
 *              11/15/11
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_file_image(hid_t file_id, void *buf_ptr, size_t buf_len)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    ssize_t     ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "i*xz", file_id, buf_ptr, buf_len);

    /* check id */
    if(H5I_FILE != H5I_get_type(file_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* get image through the VOL */
    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_FILE_IMAGE, H5AC_dxpl_id, H5_EVENT_STACK_NULL, 
                          buf_ptr, &ret_value, buf_len) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file image")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_file_image() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_config
 *
 * Purpose:     Retrieves the current automatic cache resize configuration
 *		from the metadata cache, and return it in *config_ptr.
 *
 *		Note that the version field of *config_Ptr must be correctly
 *		filled in by the caller.  This allows us to adapt for
 *		obsolete versions of the structure.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* check args */
    if((NULL == config_ptr) || (config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Bad config_ptr")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if((ret_value = H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_MDC_CONF, H5AC_dxpl_id, H5_EVENT_STACK_NULL, 
                                       config_ptr)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get mdc configuration")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fset_mdc_config
 *
 * Purpose:     Sets the current metadata cache automatic resize
 *		configuration, using the contents of the instance of
 *		H5AC_cache_config_t pointed to by config_ptr.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fset_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")
    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_SET_MDC_CONFIG, H5AC_dxpl_id, H5_EVENT_STACK_NULL, config_ptr) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "uanvle to set MDC configuration")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fset_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_hit_rate
 *
 * Purpose:     Retrieves the current hit rate from the metadata cache.
 *		This rate is the overall hit rate since the last time
 *		the hit rate statistics were reset either manually or
 *		automatically.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_hit_rate(hid_t file_id, double *hit_rate_ptr)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*d", file_id, hit_rate_ptr);

    if(NULL == hit_rate_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL hit rate pointer")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if((ret_value = H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_MDC_HR, H5AC_dxpl_id, H5_EVENT_STACK_NULL, 
                                       hit_rate_ptr)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get MDC hit rate")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_hit_rate() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_size
 *
 * Purpose:     Retrieves the maximum size, minimum clean size, current
 *		size, and current number of entries from the metadata
 *		cache associated with the specified file.  If any of
 *		the ptr parameters are NULL, the associated datum is
 *		not returned.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
    size_t *cur_size_ptr, int *cur_num_entries_ptr)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*z*z*z*Is", file_id, max_size_ptr, min_clean_size_ptr,
             cur_size_ptr, cur_num_entries_ptr);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if((ret_value = H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_MDC_SIZE, H5AC_dxpl_id, H5_EVENT_STACK_NULL, max_size_ptr, 
                                       min_clean_size_ptr, cur_size_ptr, cur_num_entries_ptr)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get MDC size")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_size() */


/*-------------------------------------------------------------------------
 * Function:    H5Freset_mdc_hit_rate_stats
 *
 * Purpose:     Reset the hit rate statistic whose current value can
 *		be obtained via the H5Fget_mdc_hit_rate() call.  Note
 *		that this statistic will also be reset once per epoch
 *		by the automatic cache resize code if it is enabled.
 *
 *		It is probably a bad idea to call this function unless
 *		you are controlling cache size from your program instead
 *		of using our cache size control code.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Freset_mdc_hit_rate_stats(hid_t file_id)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_RESET_MDC_HIT_RATE, H5AC_dxpl_id, H5_EVENT_STACK_NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't reset cache hit rate")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Freset_mdc_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_name
 *
 * Purpose:     Gets the name of the file to which object OBJ_ID belongs.
 *              If `name' is non-NULL then write up to `size' bytes into that
 *              buffer and always return the length of the entry name.
 *              Otherwise `size' is ignored and the function does not store the name,
 *              just returning the number of characters required to store the name.
 *              If an error occurs then the buffer pointed to by `name' (NULL or non-NULL)
 *              is unchanged and the function returns a negative value.
 *
 * Note:	This routine returns the name that was used to open the file,
 *		not the actual name after resolving symlinks, etc.
 *
 * Return:      Success:        The length of the file name
 *              Failure:        Negative
 *
 * Programmer:  Raymond Lu
 *              June 29, 2004
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_name(hid_t obj_id, char *name/*out*/, size_t size)
{
    H5VL_t     *vol_plugin = NULL;
    void       *obj = NULL;
    ssize_t     ret_value;
    H5I_type_t  type;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "ixz", obj_id, name, size);

    type = H5I_get_type(obj_id);
    if(H5I_FILE != type && H5I_GROUP != type && H5I_DATATYPE != type &&
       H5I_DATASET != type && H5I_ATTR != type) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    }

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_get(obj, vol_plugin, H5VL_FILE_GET_NAME, H5AC_dxpl_id, H5_EVENT_STACK_NULL,
                     type, size, name, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_name() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_info2
 *
 * Purpose:     Gets general information about the file, including:
 *		1. Get storage size for superblock extension if there is one.
 *              2. Get the amount of btree and heap storage for entries
 *                 in the SOHM table if there is one.
 *		3. The amount of free space tracked in the file.
 *
 * Return:      Success:        non-negative on success
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi
 *              July 11, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_info2(hid_t obj_id, H5F_info2_t *finfo)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    H5I_type_t  type;
    herr_t      ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", obj_id, finfo);

    /* Check args */
    if(!finfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")

    type = H5I_get_type(obj_id);
    if(H5I_FILE != type && H5I_GROUP != type && H5I_DATATYPE != type &&
       H5I_DATASET != type && H5I_ATTR != type) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    }

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if((ret_value = H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_INFO, H5AC_dxpl_id, H5_EVENT_STACK_NULL, 
                                       type, finfo)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file info")
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_info2() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_free_sections
 *
 * Purpose:     To get free-space section information for free-space manager with
 *		TYPE that is associated with file FILE_ID.
 *		If SECT_INFO is null, this routine returns the total # of free-space
 *		sections.
 *
 * Return:      Success:        non-negative, the total # of free space sections
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; July 1st, 2009
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_free_sections(hid_t file_id, H5F_mem_t type, size_t nsects,
    H5F_sect_info_t *sect_info/*out*/)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    ssize_t     ret_value;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("Zs", "iFmzx", file_id, type, nsects, sect_info);

    /* Check args */
    if(sect_info && nsects == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "nsects must be > 0")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_GET_FREE_SECTIONS, H5AC_dxpl_id, H5_EVENT_STACK_NULL, sect_info, 
                          &ret_value, type, nsects) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file free sections")
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_free_sections() */


/*-------------------------------------------------------------------------
 * Function:    H5Fclear_elink_file_cache
 *
 * Purpose:     Releases the external file cache associated with the
 *              provided file, potentially closing any cached files
 *              unless they are held open from somewhere\ else.
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Neil Fortner; December 30, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclear_elink_file_cache(hid_t file_id)
{
    H5VL_t     *vol_plugin;
    void       *obj;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* get the file object */
    if(NULL == (obj = (void *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    if(H5VL_file_optional(obj, vol_plugin, H5VL_FILE_CLEAR_ELINK_CACHE, H5AC_dxpl_id, H5_EVENT_STACK_NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release external file cache")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclear_elink_file_cache() */
