/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h"          /* This source code file is part of the H5F module */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5Aprivate.h"         /* Attributes                               */
#include "H5ACprivate.h"        /* Metadata cache                           */
#include "H5Dprivate.h"         /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Fpkg.h"             /* File access                              */
#include "H5FDprivate.h"        /* File drivers                             */
#include "H5FLprivate.h"        /* Free lists                               */
#include "H5Gprivate.h"         /* Groups                                   */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5MFprivate.h"        /* File memory management                   */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Pprivate.h"         /* Property lists                           */
#include "H5SMprivate.h"        /* Shared Object Header Messages            */
#include "H5Tprivate.h"         /* Datatypes                                */
#include "H5VLprivate.h"        /* Virtual Object Layer                     */


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

/* VOL close */
static herr_t H5F__close_file(void *file);

/* Callback for getting object counts in a file */
static int H5F__get_all_count_cb(void H5_ATTR_UNUSED *obj_ptr, hid_t H5_ATTR_UNUSED obj_id, void *key);

/* Callback for getting IDs for open objects in a file */
static int H5F__get_all_ids_cb(void H5_ATTR_UNUSED *obj_ptr, hid_t obj_id, void *key);


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

/* Declare a free list to manage the H5VL_t struct */
H5FL_EXTERN(H5VL_t);

/* Declare a free list to manage the H5VL_object_t struct */
H5FL_EXTERN(H5VL_object_t);

/* File ID class */
static const H5I_class_t H5I_FILE_CLS[1] = {{
    H5I_FILE,            /* ID class value */
    0,                   /* Class flags */
    0,                   /* # of reserved IDs for class */
    (H5I_free_t)H5F__close_file  /* Callback routine for closing objects of this class */
}};


/*--------------------------------------------------------------------------
NAME
   H5F__init_package -- Initialize interface-specific information
USAGE
    herr_t H5F__init_package()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5F__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /*
     * Initialize the atom group for the file IDs.
     */
    if(H5I_register_type(H5I_FILE_CLS) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to initialize interface")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__init_package() */


/*-------------------------------------------------------------------------
 * Function:    H5F_term_package
 *
 * Purpose:     Terminate this interface: free all memory and reset global
 *              variables to their initial values.  Release all ID groups
 *              associated with this interface.
 *
 * Return:      Success:    Positive if anything was done that might
 *                          have affected other interfaces;
 *                          zero otherwise.
 *
 *              Failure:    Never fails
 *
 *-------------------------------------------------------------------------
 */
int
H5F_term_package(void)
{
    int    n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        if(H5I_nmembers(H5I_FILE) > 0) {
            (void)H5I_clear_type(H5I_FILE, FALSE, FALSE);
            n++; /*H5I*/
        } /* end if */
        else {
            /* Make certain we've cleaned up all the shared file objects */
            H5F_sfile_assert_num(0);

            /* Destroy the file object id group */
            n += (H5I_dec_type_ref(H5I_FILE) > 0);

            /* Mark closed */
            if(0 == n)
                H5_PKG_INIT_VAR = FALSE;
        } /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5F_term_package() */


/*-------------------------------------------------------------------------
 * Function:    H5F__close_file
 *
 * Purpose:     Called when the ref count reaches zero on the file's ID
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__close_file(void *_file)
{
    H5VL_object_t   *file = (H5VL_object_t *)_file;
    herr_t          ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(_file);

    /* Close the file through the VOL */
    if ((ret_value = H5VL_file_close(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, H5_REQUEST_NULL)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close file");

    /* Free the file object */
    if (H5VL_free_object(file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDEC, FAIL, "unable to free VOL object");
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__close_file() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_create_plist
 *
 * Purpose:     Get an atom for a copy of the file-creation property list for
 *              this file. This function returns an atom with a copy of the
 *              properties used to create a file.
 *
 * Return:      Success:    Object ID for a copy of the file creation
 *                          property list.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_create_plist(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    hid_t           ret_value = H5I_INVALID_HID;    /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE1("i", "i", file_id);

    /* check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "invalid file identifier")

    /* Retrieve the file's access property list */
    if (H5VL_file_get(file->vol_obj, file->vol_info->vol_cls, H5VL_FILE_GET_FCPL, 
                     H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, H5I_INVALID_HID, "can't get file creation property list via the VOL")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_create_plist() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_access_plist
 *
 * Purpose:     Returns a copy of the file access property list of the
 *              specified file.
 *
 *              NOTE: Make sure that, if you are going to overwrite
 *              information in the copied property list that was
 *              previously opened and assigned to the property list, then
 *              you must close it before overwriting the values.
 *
 * Return:      Success:    Object ID for a copy of the file access
 *                          property list.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_access_plist(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    hid_t           ret_value = H5I_INVALID_HID;    /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE1("i", "i", file_id);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "invalid file identifier")

    /* Retrieve the file's access property list */
    if (H5VL_file_get(file->vol_obj, file->vol_info->vol_cls, H5VL_FILE_GET_FAPL, 
                     H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, H5I_INVALID_HID, "can't get file access property list via the VOL")

done:
    /* XXX: Did not move over error handling from the vol branch since it seemed superfluous */
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_access_plist() */


/*-------------------------------------------------------------------------
 * Function:    H5F__get_all_count_cb
 *
 * Purpose:     Get counter of all object types currently open.
 *
 * Return:      Success:    H5_ITER_CONT or H5_ITER_STOP
 *
 *              Failure:    H5_ITER_ERROR
 *
 *-------------------------------------------------------------------------
 */
static int
H5F__get_all_count_cb(void H5_ATTR_UNUSED *obj_ptr, hid_t H5_ATTR_UNUSED obj_id, void *key)
{
    H5F_trav_obj_cnt_t *udata = (H5F_trav_obj_cnt_t *)key;
    int                ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_STATIC_NOERR

    *(udata->obj_count) = *(udata->obj_count) + 1; 

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F_get_all_count_cb */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_obj_count
 *
 * Purpose:     Public function returning the number of opened object IDs
 *              (files, datasets, groups and datatypes) in the same file.
 *
 * Return:      Success:    The number of opened object IDs
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_obj_count(hid_t file_id, unsigned types)
{
    ssize_t  ret_value;         /* Return value */

    FUNC_ENTER_API((-1))
    H5TRACE2("Zs", "iIu", file_id, types);

    /* Check arguments */
    if (0 == (types & H5F_OBJ_ALL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, (-1), "not an object type")

    /* Perform the query */
    /* If the 'special' ID wasn't passed in, just make a normal VOL call to
     * count the IDs in the file.
     */
    if (file_id != (hid_t)H5F_OBJ_ALL) {
        H5VL_object_t *obj;

        /* get the file object */
        if (NULL == (obj = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "not a file id")

        /* Get the count via the VOL driver */
        if (H5VL_file_get(obj->vol_obj, obj->vol_info->vol_cls, H5VL_FILE_GET_OBJ_COUNT, 
                         H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, types, &ret_value) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get object count in file(s)")
    }
    /* If we passed in the 'special' ID, get the count for everything open in the
     * library, iterating over all open files and getting the object count for each.
     *
     * XXX: Consider making this a helper function in H5I.
     * XXX: Should probably have made a special hid_t value and not overloaded
     *      the 'all flags' value.
     */
    else {
        H5F_trav_obj_cnt_t udata;

        udata.obj_count = &ret_value;
        udata.types = types | H5F_OBJ_LOCAL;

        if (types & H5F_OBJ_FILE) {
            if (H5I_iterate(H5I_FILE, H5F__get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over file IDs failed");
        }
        if (types & H5F_OBJ_DATASET) {
            if (H5I_iterate(H5I_DATASET, H5F__get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over dataset IDs failed");
        }
        if (types & H5F_OBJ_GROUP) {
            if (H5I_iterate(H5I_GROUP, H5F__get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over group IDs failed");
        }
        if (types & H5F_OBJ_DATATYPE) {
            if (H5I_iterate(H5I_DATATYPE, H5F__get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over datatype IDs failed");
        }
        if (types & H5F_OBJ_ATTR) {
            if (H5I_iterate(H5I_ATTR, H5F__get_all_count_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over attribute IDs failed");
        }
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_obj_count() */


/*-------------------------------------------------------------------------
 * Function:    H5F__get_all_ids_cb
 *
 * Purpose:     Get IDs of all currently open objects of a given type.
 *
 * Return:      Success:    H5_ITER_CONT or H5_ITER_STOP
 *
 *              Failure:    H5_ITER_ERROR
 *
 *-------------------------------------------------------------------------
 */
static int
H5F__get_all_ids_cb(void H5_ATTR_UNUSED *obj_ptr, hid_t obj_id, void *key)
{
    H5F_trav_obj_ids_t *udata = (H5F_trav_obj_ids_t *)key;
    int                ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_STATIC_NOERR

    if (*udata->obj_count >= udata->max_objs)
        HGOTO_DONE(H5_ITER_STOP);

    udata->oid_list[*udata->obj_count] = obj_id;
    *(udata->obj_count) = *(udata->obj_count) + 1;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__get_all_ids_cb */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_object_ids
 *
 * Purpose:     Public function to return a list of opened object IDs.
 *
 * Return:      Success:    The number of IDs in oid_list
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *oid_list)
{
    /* XXX: Note the type mismatch - you can ask for more objects than can be returned */

    ssize_t     ret_value;          /* Return value */

    FUNC_ENTER_API((-1))
    H5TRACE4("Zs", "iIuz*i", file_id, types, max_objs, oid_list);

    /* Check arguments */
    if (0 == (types & H5F_OBJ_ALL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, (-1), "not an object type")
    if (!oid_list)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, (-1), "object ID list cannot be NULL")

    /* Perform the query */
    /* If the 'special' ID wasn't passed in, just make a normal VOL call to
     * get the IDs from the file.
     */
    if (file_id != (hid_t)H5F_OBJ_ALL) {
        H5VL_object_t *file;

        /* get the file object */
        if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "invalid file identifier")

        /* Get the IDs via the VOL driver */
        if (H5VL_file_get(file->vol_obj, file->vol_info->vol_cls, H5VL_FILE_GET_OBJ_IDS, 
                         H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, types, max_objs, oid_list, &ret_value) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get object count in file(s)")
    }
    /* If we passed in the 'special' ID, get the count for everything open in the
     * library, iterating over all open files and getting the object count for each.
     *
     * XXX: Consider making this a helper function in H5I.
     * XXX: Should probably have made a special hid_t value and not overloaded
     *      the 'all flags' value.
     * XXX: Note that the RM states that passing in a negative value for max_objs
     *      gets you all the objects. This technically works, but is clearly wrong
     *      behavior since max_objs is an unsigned type.
     */
    else {
        H5F_trav_obj_ids_t udata;

        /* XXX: Unclear why this is commented out */
        //udata.types = types | H5F_OBJ_LOCAL;
        udata.max_objs = max_objs;
        udata.oid_list = oid_list;
        udata.obj_count = &ret_value;

        if (types & H5F_OBJ_FILE) {
            if (H5I_iterate(H5I_FILE, H5F__get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over file IDs failed");
        }
        if (types & H5F_OBJ_DATASET) {
            if (H5I_iterate(H5I_DATASET, H5F__get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over dataset IDs failed");
        }
        if (types & H5F_OBJ_GROUP) {
            if (H5I_iterate(H5I_GROUP, H5F__get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over group IDs failed");
        }
        if (types & H5F_OBJ_DATATYPE) {
            if (H5I_iterate(H5I_DATATYPE, H5F__get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over datatype IDs failed");
        }
        if (types & H5F_OBJ_ATTR) {
            if (H5I_iterate(H5I_ATTR, H5F__get_all_ids_cb, &udata, TRUE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_BADITER, (-1), "iteration over attribute IDs failed");
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
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_vfd_handle(hid_t file_id, hid_t fapl_id, void **file_handle)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ii**x", file_id, fapl_id, file_handle);

    /* Check args */
    if (!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file handle pointer")

    /* Get the file object */
    if (NULL == (file = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Retrieve the VFD handle for the file */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_VFD_HANDLE, file_handle, fapl_id) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_vfd_handle() */


/*-------------------------------------------------------------------------
 * Function:    H5Fis_accessible
 *
 * Purpose:     Check if the file can be opened with the given fapl.
 *
 * Return:      Success:    1 (true) or 0 (false)
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Fis_accessible(const char *name, hid_t fapl_id)
{
    htri_t      ret_value;              /* Return value */

    FUNC_ENTER_API((-1))
    H5TRACE2("t", "*si", name, fapl_id);

    /* Check args */
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, (-1), "no file name specified")

    /* Check the file access property list */
    if (H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if (TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "not file access property list")

    /* Call into the VOL to check if file is accessible */
    if (H5VL_file_specific(NULL, NULL, H5VL_FILE_IS_ACCESSIBLE, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, fapl_id, name, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fis_accessible() */


/*-------------------------------------------------------------------------
 * Function:    H5Fcreate
 *
 * Purpose:     This is the primary function for creating HDF5 files . The
 *              flags parameter determines whether an existing file will be
 *              overwritten or not.  All newly created files are opened for
 *              both reading and writing.  All flags may be combined with the
 *              bit-wise OR operator (`|') to change the behavior of the file
 *              create call.
 *
 *              The more complex behaviors of a file's creation and access
 *              are controlled through the file-creation and file-access
 *              property lists.  The value of H5P_DEFAULT for a template
 *              value indicates that the library should use the default
 *              values for the appropriate template.
 *
 * See also:    H5Fpublic.h for the list of supported flags. H5Ppublic.h for
 *              the list of file creation and file access properties.
 *
 * Return:      Success:    A file ID
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    void               *new_file = NULL;    /* file struct for new file                 */

    H5P_genplist_t     *plist;              /* Property list pointer                    */
    H5VL_class_t       *vol_cls = NULL;     /* VOL Class structure for callback info    */
    H5VL_t             *vol_info = NULL;    /* VOL info struct                          */
    H5VL_driver_prop_t  driver_prop;        /* Property for vol driver ID & info        */

    hid_t               dxpl_id = H5AC_ind_read_dxpl_id;    /* dxpl used by library     */
    hid_t               ret_value;          /* return value                             */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE4("i", "*sIuii", filename, flags, fcpl_id, fapl_id);

    /* Check/fix arguments */
    if (!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "invalid file name")

    /* In this routine, we only accept the following flags:
     *          H5F_ACC_EXCL, H5F_ACC_TRUNC and H5F_ACC_SWMR_WRITE
     */
    if (flags & ~(H5F_ACC_EXCL | H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "invalid flags")

    /* The H5F_ACC_EXCL and H5F_ACC_TRUNC flags are mutually exclusive */
    if ((flags & H5F_ACC_EXCL) && (flags & H5F_ACC_TRUNC))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "mutually exclusive flags for file creation")

    /* Check file creation property list */
    if (H5P_DEFAULT == fcpl_id)
        fcpl_id = H5P_FILE_CREATE_DEFAULT;
    else
        if (TRUE != H5P_isa_class(fcpl_id, H5P_FILE_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not file create property list")

    /* Verify access property list and get correct dxpl */
    if (H5P_verify_apl_and_dxpl(&fapl_id, H5P_CLS_FACC, &dxpl_id, H5I_INVALID_HID, TRUE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, H5I_INVALID_HID, "can't set access and transfer property lists")

    /* get the VOL info from the fapl */
    if (NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a file access property list")
    if (H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't get vol driver info")
    if (NULL == (vol_cls = (H5VL_class_t *)H5I_object_verify(driver_prop.driver_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a VOL driver ID")

    /* Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if (0 == (flags & (H5F_ACC_EXCL | H5F_ACC_TRUNC)))
        flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /* create a new file or truncate an existing file through the VOL */
    if (NULL == (new_file = H5VL_file_create(vol_cls, filename, flags, fcpl_id, fapl_id, 
                                        dxpl_id, H5_REQUEST_NULL)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, H5I_INVALID_HID, "unable to create file")

    /* setup VOL info struct */
    if (NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, H5I_INVALID_HID, "can't allocate VL info struct")
    vol_info->vol_cls = vol_cls;
    vol_info->vol_id = driver_prop.driver_id;
    if (H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL driver")

    /* Get an atom for the file */
    if ((ret_value = H5VL_register_id(H5I_FILE, new_file, vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fcreate() */


/*-------------------------------------------------------------------------
 * Function:    H5Fopen
 *
 * Purpose:     This is the primary function for accessing existing HDF5
 *              files.  The FLAGS argument determines whether writing to an
 *              existing file will be allowed or not.  All flags may be
 *              combined with the bit-wise OR operator (`|') to change the
 *              behavior of the file open call.  The more complex behaviors
 *              of a file's access are controlled through the file-access
 *              property list.
 *
 * See Also:    H5Fpublic.h for a list of possible values for FLAGS.
 *
 * Return:      Success:    A file ID
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen(const char *filename, unsigned flags, hid_t fapl_id)
{
    void               *new_file = NULL;            /* file struct for new file                 */
    hid_t               dxpl_id = H5AC_ind_read_dxpl_id;    /* dxpl used by library             */
    H5P_genplist_t     *plist;                      /* Property list pointer                    */
    H5VL_driver_prop_t  driver_prop;                /* Property for vol driver ID & info        */
    H5VL_class_t       *vol_cls = NULL;             /* VOL Class structure for callback info    */
    H5VL_t             *vol_info = NULL;            /* VOL info struct                          */
    hid_t               ret_value;                  /* return value                             */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE3("i", "*sIui", filename, flags, fapl_id);

    /* Check/fix arguments. */
    if (!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "invalid file name")
    /* Reject undefined flags (~H5F_ACC_PUBLIC_FLAGS) and the H5F_ACC_TRUNC & H5F_ACC_EXCL flags */
    if ((flags & ~H5F_ACC_PUBLIC_FLAGS) ||
            (flags & H5F_ACC_TRUNC) || (flags & H5F_ACC_EXCL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5I_INVALID_HID, "invalid file open flags")
    /* XXX: Might want to move SWMR flag checks to H5F_open() */
    /* Asking for SWMR write access on a read-only file is invalid */
    if ((flags & H5F_ACC_SWMR_WRITE) && 0 == (flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, H5I_INVALID_HID, "SWMR write access on a file open for read-only access is not allowed")
    /* Asking for SWMR read access on a non-read-only file is invalid */
    if ((flags & H5F_ACC_SWMR_READ) && (flags & H5F_ACC_RDWR))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, H5I_INVALID_HID, "SWMR read access on a file open for read-write access is not allowed")

    /* Verify access property list and get correct dxpl */
    if (H5P_verify_apl_and_dxpl(&fapl_id, H5P_CLS_FACC, &dxpl_id, H5I_INVALID_HID, TRUE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, H5I_INVALID_HID, "can't set access and transfer property lists")

    /* get the VOL info from the fapl */
    if (NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a file access property list")
    if (H5P_peek(plist, H5F_ACS_VOL_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5I_INVALID_HID, "can't get VOL driver info")
    if (NULL == (vol_cls = (H5VL_class_t *)H5I_object_verify(driver_prop.driver_id, H5I_VOL)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "invalid VOL driver ID")

    /* Open the file through the VOL layer */
    if (NULL == (new_file = H5VL_file_open(vol_cls, filename, flags, fapl_id, dxpl_id, H5_REQUEST_NULL)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, H5I_INVALID_HID, "unable to create file")

    /* setup VOL info struct */
    if (NULL == (vol_info = H5FL_CALLOC(H5VL_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, H5I_INVALID_HID, "can't allocate VL info struct")
    vol_info->vol_cls = vol_cls;
    vol_info->vol_id = driver_prop.driver_id;
    if (H5I_inc_ref(vol_info->vol_id, FALSE) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VOL driver")

    /* Get an atom for the file */
    if ((ret_value = H5VL_register_id(H5I_FILE, new_file, vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize file handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fopen() */


/*-------------------------------------------------------------------------
 * Function:    H5Fflush
 *
 * Purpose:     Flushes all outstanding buffers of a file to disk but does
 *              not remove them from the cache.  The OBJECT_ID can be a file,
 *              dataset, group, attribute, or named data type.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fflush(hid_t object_id, H5F_scope_t scope)
{
    H5VL_object_t   *obj = NULL;                    /* Object info      */
    H5I_type_t      obj_type;                       /* Type of object   */
    herr_t          ret_value = SUCCEED;            /* Return value     */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iFs", object_id, scope);

    /* Get the type of object we're flushing + sanity check */
    obj_type = H5I_get_type(object_id);
    if (H5I_FILE != obj_type && H5I_GROUP != obj_type && H5I_DATATYPE != obj_type &&
       H5I_DATASET != obj_type && H5I_ATTR != obj_type) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    }

    /* get the file object */
    if (NULL == (obj = H5VL_get_object(object_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid object identifier")

    /* Flush the object */
    if (H5VL_file_specific(obj->vol_obj, obj->vol_info->vol_cls, H5VL_FILE_FLUSH, 
                          H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, obj_type, scope) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fflush() */


/*-------------------------------------------------------------------------
 * Function:    H5Fclose
 *
 * Purpose:     This function closes the file specified by FILE_ID by
 *              flushing all data to storage, and terminating access to the
 *              file through FILE_ID.  If objects (e.g., datasets, groups,
 *              etc.) are open in the file then the underlying storage is not
 *              closed until those objects are closed; however, all data for
 *              the file and the open objects is flushed.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclose(hid_t file_id)
{
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check/fix arguments. */
    if (H5I_FILE != H5I_get_type(file_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID")

    /* Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if (H5I_dec_app_ref(file_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclose() */


/*-------------------------------------------------------------------------
 * Function:    H5Freopen
 *
 * Purpose:     Reopen a file.  The new file handle which is returned points
 *              to the same file as the specified file handle.  Both handles
 *              share caches and other information.  The only difference
 *              between the handles is that the new handle is not mounted
 *              anywhere and no files are mounted on it.
 *
 * Return:      Success:    New file ID
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Freopen(hid_t file_id)
{
    H5VL_object_t   *obj = NULL;
    void            *file;                          /* File token from VOL driver */
    hid_t           ret_value = H5I_INVALID_HID;    /* Return value */

    FUNC_ENTER_API(H5I_INVALID_HID)
    H5TRACE1("i", "i", file_id);

    /* Get the file object */
    if (NULL == (obj = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "invalid file identifier")

    /* Reopen the file */
    if (H5VL_file_optional(obj->vol_obj, obj->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_REOPEN, &file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, H5I_INVALID_HID, "unable to reopen file via the VOL driver")

    /* Make sure that worked */
    if (NULL == file)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, H5I_INVALID_HID, "unable to reopen file")

    /* Get an atom for the file */
    if ((ret_value = H5VL_register_id(H5I_FILE, file, obj->vol_info, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to atomize file handle")

done:
    /* XXX: If registration fails, file will not be closed */
    FUNC_LEAVE_API(ret_value)
} /* end H5Freopen() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_intent
 *
 * Purpose:     Public API to retrieve the file's 'intent' flags passed
 *              during H5Fopen()
 *
 * Return:      SUCCEED/FAIL
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
    /* XXX: Why do we silently handle NULL values in this way? */
    if (intent_flags) {
        H5VL_object_t   *file;                      /* File info */

        /* Get the internal file structure */
        if (NULL == (file = (H5VL_object_t *)H5I_object(file_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

        /* Get the flags */
        if ((ret_value = H5VL_file_get(file->vol_obj, file->vol_info->vol_cls, H5VL_FILE_GET_INTENT, 
                                      H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, intent_flags)) < 0)
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
 * Return:      Success:    Amount of free space for type
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5Fget_freespace(hid_t file_id)
{
    H5VL_object_t *obj = NULL;
    hssize_t    ret_value;      /* Return value */

    FUNC_ENTER_API((-1))
    H5TRACE1("Hs", "i", file_id);

    /* get the file object */
    if(NULL == (obj = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "invalid file identifier")

    /* Go get the actual amount of free space in the file */
    if(H5VL_file_optional(obj->vol_obj, obj->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_FREE_SPACE, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get file free space")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_freespace() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_filesize
 *
 * Purpose:     Retrieves the file size of the HDF5 file. This function
 *              is called after an existing file is opened in order
 *              to learn the true size of the underlying file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_filesize(hid_t file_id, hsize_t *size)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", file_id, size);

    /* Check args */
    if (!size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "size parameter cannot be NULL")
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Get the file size */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_SIZE, size) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_filesize() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_file_image
 *
 * Purpose:     If a buffer is provided (via the buf_ptr argument) and is
 *              big enough (size in buf_len argument), load *buf_ptr with
 *              an image of the open file whose ID is provided in the
 *              file_id parameter, and return the number of bytes copied
 *              to the buffer.
 *
 *              If the buffer exists, but is too small to contain an image
 *              of the indicated file, return a negative number.
 *
 *              Finally, if no buffer is provided, return the size of the
 *              buffer needed.  This value is simply the eoa of the target
 *              file.
 *
 *              Note that any user block is skipped.
 *
 *              Also note that the function may not be used on files
 *              opened with either the split/multi file driver or the
 *              family file driver.
 *
 *              In the former case, the sparse address space makes the
 *              get file image operation impractical, due to the size of
 *              the image typically required.
 *
 *              In the case of the family file driver, the problem is
 *              the driver message in the super block, which will prevent
 *              the image being opened with any driver other than the
 *              family file driver -- which negates the purpose of the
 *              operation.  This can be fixed, but no resources for
 *              this now.
 *
 * Return:      Success:    Bytes copied / number of bytes needed
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_file_image(hid_t file_id, void *buf_ptr, size_t buf_len)
{
    H5VL_object_t  *file;           /* File object for file ID  */
    ssize_t         ret_value;      /* Return value             */

    FUNC_ENTER_API((-1))
    H5TRACE3("Zs", "i*xz", file_id, buf_ptr, buf_len);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, (-1), "not a file ID")

    /* Get image through the VOL */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, 
                          H5VL_FILE_GET_FILE_IMAGE, buf_ptr, &ret_value, buf_len) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get file image")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_file_image() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_config
 *
 * Purpose:     Retrieves the current automatic cache resize configuration
 *              from the metadata cache, and return it in *config_ptr.
 *
 *              Note that the version field of *config_Ptr must be correctly
 *              filled in by the caller.  This allows us to adapt for
 *              obsolete versions of the structure.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
{
    H5VL_object_t *obj = NULL;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* Check args */
    if ((NULL == config_ptr) || (config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Bad config_ptr")

    /* Get the file object */
    if (NULL == (obj = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")


    /* Get the metadata cache configuration */
    if (H5VL_file_optional(obj->vol_obj, obj->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_MDC_CONF, config_ptr) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get mdc configuration")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fset_mdc_config
 *
 * Purpose:     Sets the current metadata cache automatic resize
 *              configuration, using the contents of the instance of
 *              H5AC_cache_config_t pointed to by config_ptr.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fset_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
{
    H5VL_object_t *obj = NULL;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* Get the file object */
    if (NULL == (obj = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Set the metadata cache configuration  */
    if (H5VL_file_optional(obj->vol_obj, obj->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_SET_MDC_CONFIG, config_ptr) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "uanvle to set MDC configuration")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fset_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_hit_rate
 *
 * Purpose:     Retrieves the current hit rate from the metadata cache.
 *              This rate is the overall hit rate since the last time
 *              the hit rate statistics were reset either manually or
 *              automatically.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_hit_rate(hid_t file_id, double *hit_rate_ptr)
{
    H5VL_object_t       *file;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*d", file_id, hit_rate_ptr);

    /* Check args */
    if (NULL == hit_rate_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL hit rate pointer")
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Get the current hit rate */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_MDC_HR, hit_rate_ptr) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get MDC hit rate")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_hit_rate() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_size
 *
 * Purpose:     Retrieves the maximum size, minimum clean size, current
 *              size, and current number of entries from the metadata
 *              cache associated with the specified file.  If any of
 *              the ptr parameters are NULL, the associated datum is
 *              not returned.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
    size_t *cur_size_ptr, int *cur_num_entries_ptr)
{
    H5VL_object_t       *file;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*z*z*z*Is", file_id, max_size_ptr, min_clean_size_ptr,
             cur_size_ptr, cur_num_entries_ptr);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Get the size data */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_MDC_SIZE, 
                          max_size_ptr, min_clean_size_ptr, cur_size_ptr, cur_num_entries_ptr) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get MDC size")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_size() */


/*-------------------------------------------------------------------------
 * Function:    H5Freset_mdc_hit_rate_stats
 *
 * Purpose:     Reset the hit rate statistic whose current value can
 *              be obtained via the H5Fget_mdc_hit_rate() call.  Note
 *              that this statistic will also be reset once per epoch
 *              by the automatic cache resize code if it is enabled.
 *
 *              It is probably a bad idea to call this function unless
 *              you are controlling cache size from your program instead
 *              of using our cache size control code.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Freset_mdc_hit_rate_stats(hid_t file_id)
{
    H5VL_object_t *obj = NULL;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* get the file object */
    if (NULL == (obj = (H5VL_object_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Reset the hit rate statistic */
    if (H5VL_file_optional(obj->vol_obj, obj->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_RESET_MDC_HIT_RATE) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't reset cache hit rate")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Freset_mdc_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_name
 *
 * Purpose:     Gets the name of the file to which object OBJ_ID belongs.
 *              If 'name' is non-NULL then write up to 'size' bytes into that
 *              buffer and always return the length of the entry name.
 *              Otherwise `size' is ignored and the function does not store
 *              the name, just returning the number of characters required to
 *              store the name. If an error occurs then the buffer pointed to
 *              by 'name' (NULL or non-NULL) is unchanged and the function
 *              returns a negative value.
 *
 * Note:        This routine returns the name that was used to open the file,
 *              not the actual name after resolving symlinks, etc.
 *
 * Return:      Success:    The length of the file name
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_name(hid_t obj_id, char *name/*out*/, size_t size)
{
    H5VL_object_t       *obj = NULL;
    H5I_type_t          type;
    ssize_t             ret_value = -1;

    FUNC_ENTER_API((-1))
    H5TRACE3("Zs", "ixz", obj_id, name, size);

    /* Check the type */
    type = H5I_get_type(obj_id);
    if (H5I_FILE != type && H5I_GROUP != type && H5I_DATATYPE != type && H5I_DATASET != type && H5I_ATTR != type)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "not a file or file object")

    /* Get the file object */
    if (NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "invalid file identifier")

    /* Get the filename via the VOL */
    if (H5VL_file_get(obj->vol_obj, obj->vol_info->vol_cls, H5VL_FILE_GET_NAME, 
                     H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, type, size, name, &ret_value) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get file name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_name() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_info2
 *
 * Purpose:     Gets general information about the file, including:
 *              1. Get storage size for superblock extension if there is one.
 *              2. Get the amount of btree and heap storage for entries
 *                 in the SOHM table if there is one.
 *              3. The amount of free space tracked in the file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_info2(hid_t obj_id, H5F_info2_t *finfo)
{
    H5VL_object_t  *obj = NULL;
    H5I_type_t      type;
    herr_t          ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", obj_id, finfo);

    /* Check args */
    if (!finfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file info pointer can't be NULL")

    /* Check the type */
    type = H5I_get_type(obj_id);
    if (H5I_FILE != type && H5I_GROUP != type && H5I_DATATYPE != type && H5I_DATASET != type && H5I_ATTR != type)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

    /* Get the file object */
    if (NULL == (obj = H5VL_get_object(obj_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Get the file information via the VOL */
    if (H5VL_file_optional(obj->vol_obj, obj->vol_info->vol_cls, H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, 
                          H5VL_FILE_GET_INFO, type, finfo) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "unable to get file info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_info2() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_metadata_read_retry_info
 *
 * Purpose:     To retrieve the collection of read retries for metadata
 *              items with checksum.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_metadata_read_retry_info(hid_t file_id, H5F_retry_info_t *info)
{
    H5VL_object_t   *file       = NULL;         /* File object for file ID */
    herr_t          ret_value   = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, info);

    /* Check args */
    if (!info)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")

    /* Get the file pointer */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Get the retry info */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_METADATA_READ_RETRY_INFO, info) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't get metadata read retry info")


done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_metadata_read_retry_info() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_free_sections
 *
 * Purpose:     To get free-space section information for free-space manager with
 *              TYPE that is associated with file FILE_ID.
 *              If SECT_INFO is null, this routine returns the total # of free-space
 *              sections.
 *
 * Return:      Success:   The total # of free space sections
 *
 *              Failure:   -1
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_free_sections(hid_t file_id, H5F_mem_t type, size_t nsects,
    H5F_sect_info_t *sect_info/*out*/)
{
    H5VL_object_t  *file        = NULL;
    ssize_t         ret_value   = -1;       /* Return value */

    FUNC_ENTER_API((-1))
    H5TRACE4("Zs", "iFmzx", file_id, type, nsects, sect_info);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, (-1), "invalid file identifier")
    if (sect_info && nsects == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, (-1), "nsects must be > 0")

    /* Go get the free-space section information in the file */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, H5_REQUEST_NULL, 
                          H5VL_FILE_GET_FREE_SECTIONS, sect_info, &ret_value, type, nsects) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, (-1), "unable to get file free sections")

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
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclear_elink_file_cache(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Release the EFC */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_CLEAR_ELINK_CACHE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release external file cache")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclear_elink_file_cache() */


/*-------------------------------------------------------------------------
 * Function:    H5Fstart_swmr_write
 *
 * Purpose:     To enable SWMR writing mode for the file
 *
 *              1) Refresh opened objects: part 1
 *              2) Flush & reset accumulator
 *              3) Mark the file in SWMR writing mode
 *              4) Set metadata read attempts and retries info
 *              5) Disable accumulator
 *              6) Evict all cache entries except the superblock
 *              7) Refresh opened objects (part 2)
 *              8) Unlock the file
 *
 *              Pre-conditions:
 *
 *              1) The file being opened has v3 superblock
 *              2) The file is opened with H5F_ACC_RDWR
 *              3) The file is not already marked for SWMR writing
 *              4) Current implementaion for opened objects:
 *                  --only allow datasets and groups without attributes
 *                  --disallow named datatype with/without attributes
 *                  --disallow opened attributes attached to objects
 *
 * NOTE:        Currently, only opened groups and datasets are allowed
 *              when enabling SWMR via H5Fstart_swmr_write().
 *              Will later implement a different approach--
 *              set up flush dependency/proxy even for file opened without
 *              SWMR to resolve issues with opened objects.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fstart_swmr_write(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "hid_t identifier is not a file ID")

    /* start SWMR writing */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_START_SWMR_WRITE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "unable to start SWMR writing")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fstart_swmr_write() */


/*-------------------------------------------------------------------------
 * Function:    H5Fstart_mdc_logging
 *
 * Purpose:     Start metadata cache logging operations for a file.
 *                  - Logging must have been set up via the fapl.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fstart_mdc_logging(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Sanity check */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "hid_t identifier is not a file ID")

    /* Call mdc logging function */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_START_MDC_LOGGING) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_LOGFAIL, FAIL, "unable to start mdc logging")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fstart_mdc_logging() */


/*-------------------------------------------------------------------------
 * Function:    H5Fstop_mdc_logging
 *
 * Purpose:     Stop metadata cache logging operations for a file.
 *                  - Does not close the log file.
 *                  - Logging must have been set up via the fapl.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fstop_mdc_logging(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Sanity check */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "hid_t identifier is not a file ID")

    /* Call mdc logging function */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_STOP_MDC_LOGGING) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_LOGFAIL, FAIL, "unable to stop mdc logging")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fstop_mdc_logging() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_logging_status
 *
 * Purpose:     Get the logging flags. is_enabled determines if logging was
 *              set up via the fapl. is_currently_logging determines if
 *              log messages are being recorded at this time.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_logging_status(hid_t file_id, hbool_t *is_enabled,
                          hbool_t *is_currently_logging)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*b*b", file_id, is_enabled, is_currently_logging);

    /* Sanity check */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "hid_t identifier is not a file ID")

    /* Call mdc logging function */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_MDC_LOGGING_STATUS, is_enabled, is_currently_logging) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_LOGFAIL, FAIL, "unable to get logging status")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_logging_status() */


/*-------------------------------------------------------------------------
 * Function:    H5Fset_latest_format
 *
 * Purpose:     Enable switching the "latest format" flag while a file is open.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fset_latest_format(hid_t file_id, hbool_t latest_format)
{
    H5F_t *f;                           /* File */
    unsigned latest_flags;              /* Latest format flags for file */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", file_id, latest_format);

    /* Check args */
    if (NULL == (f = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "not a file ID")

    /* Check if the value is changing */
    latest_flags = H5F_USE_LATEST_FLAGS(f, H5F_LATEST_ALL_FLAGS);
    if (latest_format != (H5F_LATEST_ALL_FLAGS == latest_flags)) {
        /* Call the flush routine, for this file */
        if (H5F__flush(f, H5AC_ind_read_dxpl_id, H5AC_rawdata_dxpl_id, FALSE) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")

        /* Toggle the 'latest format' flag */
        H5F_SET_LATEST_FLAGS(f, latest_format ? H5F_LATEST_ALL_FLAGS : 0);
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fset_latest_format() */


/*-------------------------------------------------------------------------
 * Function:    H5Fformat_convert_super (Internal)
 *
 * Purpose:     Downgrade the superblock version to v2 and
 *              downgrade persistent file space to non-persistent
 *              for 1.8 library.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fformat_convert(hid_t fid)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fid);

    if (H5I_FILE == H5I_get_type(fid)) {
        H5F_t    *f;                     /* File to flush */
        hbool_t   mark_dirty = FALSE;

        /* Get file object */
        if (NULL == (f = (H5F_t *)H5I_object(fid)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

        /* Check if the superblock should be downgraded */
        if (f->shared->sblock->super_vers > HDF5_SUPERBLOCK_VERSION_V18_LATEST) {
            f->shared->sblock->super_vers = HDF5_SUPERBLOCK_VERSION_V18_LATEST;
            mark_dirty = TRUE;
        }

        /* Check for persistent freespace manager, which needs to be downgraded */
        if (!(f->shared->fs_strategy == H5F_FILE_SPACE_STRATEGY_DEF &&
                f->shared->fs_persist == H5F_FREE_SPACE_PERSIST_DEF &&
                f->shared->fs_threshold == H5F_FREE_SPACE_THRESHOLD_DEF &&
                f->shared->fs_page_size == H5F_FILE_SPACE_PAGE_SIZE_DEF)) {
            /* Check to remove free-space manager info message from superblock extension */
            if (H5F_addr_defined(f->shared->sblock->ext_addr))
                if (H5F_super_ext_remove_msg(f, H5AC_ind_read_dxpl_id, H5O_FSINFO_ID) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "error in removing message from superblock extension")

            /* Close freespace manager */
            if (H5MF_try_close(f, H5AC_ind_read_dxpl_id) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "unable to free free-space address")

            /* Set non-persistent freespace manager */
            f->shared->fs_strategy = H5F_FILE_SPACE_STRATEGY_DEF;
            f->shared->fs_persist = H5F_FREE_SPACE_PERSIST_DEF;
            f->shared->fs_threshold = H5F_FREE_SPACE_THRESHOLD_DEF;
            f->shared->fs_page_size = H5F_FILE_SPACE_PAGE_SIZE_DEF;

            /* Indicate that the superblock should be marked dirty */
            mark_dirty = TRUE;
        }

        /* Check if we should mark the superblock dirty */
        if (mark_dirty)
            /* Mark superblock as dirty */
            if (H5F_super_dirty(f) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, FAIL, "unable to mark superblock as dirty")
    }
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fformat_convert() */


/*-------------------------------------------------------------------------
 * Function:    H5Freset_page_buffering_stats
 *
 * Purpose:     Resets statistics for the page buffer layer.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Freset_page_buffering_stats(hid_t file_id)
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* Reset the statistics */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_RESET_PAGE_BUFFERING_STATS) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't reset stats for page buffering")

done:
    FUNC_LEAVE_API(ret_value)
}   /* H5Freset_page_buffering_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_page_buffering_stats
 *
 * Purpose:     Retrieves statistics for the page buffer layer.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_page_buffering_stats(hid_t file_id, unsigned accesses[2], unsigned hits[2],
    unsigned misses[2], unsigned evictions[2], unsigned bypasses[2])
{
    H5VL_object_t   *file;                          /* File info */
    herr_t          ret_value = SUCCEED;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "i*Iu*Iu*Iu*Iu*Iu", file_id, accesses, hits, misses, evictions,
             bypasses);

    /* Check args */
    if (NULL == (file = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    if (NULL == accesses || NULL == hits || NULL == misses || NULL == evictions || NULL == bypasses)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL input parameters for stats")

    /* Get the statistics */
    if (H5VL_file_optional(file->vol_obj, file->vol_info->vol_cls, H5AC_ind_read_dxpl_id, 
                          H5_REQUEST_NULL, H5VL_FILE_GET_PAGE_BUFFERING_STATS,
                          accesses, hits, misses, evictions, bypasses) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve stats for page buffering")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_page_buffering_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_image_info
 *
 * Purpose:     Retrieves the image_addr and image_len for the cache image in the file.
 *              image_addr:  --base address of the on disk metadata cache image
 *                           --HADDR_UNDEF if no cache image
 *              image_len:   --size of the on disk metadata cache image
 *                           --zero if no cache image
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_image_info(hid_t file_id, haddr_t *image_addr, hsize_t *image_len)
{
    H5F_t      *file;                   /* File object for file ID */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*a*h", file_id, image_addr, image_len);

    /* Check args */
    if (NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    if (NULL == image_addr || NULL == image_len)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL image addr or image len")

    /* Go get the address and size of the cache image */
    if (H5AC_get_mdc_image_info(file->shared->cache, image_addr, image_len) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTGET, FAIL, "can't retrieve cache image info")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_image_info() */

